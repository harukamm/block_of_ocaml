from os import listdir
from os.path import isfile, join
from xml.etree import ElementTree
import sys, subprocess
import re

if not isfile('block_of_ocaml'):
  print 'File \'block_of_ocaml\' does not exist.'
  sys.exit()

output_bad_xml = len(sys.argv) == 2

test_path = 'tests'
results = [[] for x in xrange(3)]

STATUS_SUCCESS = 0
STATUS_ERROR_XML_PARSING = 1
STATUS_ERROR_XML_DECODING = 2

def status_name(status):
  if status == STATUS_SUCCESS:
    return "success"
  elif status == STATUS_ERROR_XML_PARSING:
    return "xml-paring-error"
  elif status == STATUS_ERROR_XML_DECODING:
    return "xml-decoding-error"
  else:
    return "unknown status"

def push_result(code, status, output, error=None):
  result = {
    "code": code,
    "status": status,
    "output": output,
    "error": error
  }
  results[status].append(result)

def output_result(result):
  print '> ' + result['code']
  status = result['status']
  if status == STATUS_SUCCESS:
    pass
  elif status == STATUS_ERROR_XML_PARSING:
    row, column = result['error'].position
    output = result['output']
    output_lines = output.split('\n')
    errored = output_lines[row - 1][column - 1:]
    print ' failed at: ' + errored
    if output_bad_xml:
      #output = output.replace('\n', '')
      #output = re.sub(r'\s+', ' ', output)
      print output
  elif status == STATUS_ERROR_XML_DECODING:
    error = result['error']
    print error.output

def output_all_results():
  for status in xrange(len(results)):
    print status_name(status)
    for re in results[status]:
      output_result(re)
    print ''

def run_test(filename):
  source = test_path + '/' + filename
  file = open(source, 'r')
  code = file.read()
  code = code.replace('\n', '')

  try:
    re = subprocess.check_output(["./block_of_ocaml", source],
        stderr=subprocess.STDOUT)
    try:
      xml_root = ElementTree.fromstring(re)
      push_result(code, STATUS_SUCCESS, xml_root)
    except ElementTree.ParseError as e:
      push_result(code, STATUS_ERROR_XML_PARSING, re, e)
  except subprocess.CalledProcessError as e:
      push_result(code, STATUS_ERROR_XML_DECODING, None, e)

def run_all_tests():
  test_files = [f for f in listdir(test_path) if isfile(join(test_path, f))]
  for filename in test_files:
    run_test(filename)

run_all_tests()
output_all_results()
