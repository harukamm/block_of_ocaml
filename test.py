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
    return "SUCCESS"
  elif status == STATUS_ERROR_XML_PARSING:
    return "failed: xml-paring-error"
  elif status == STATUS_ERROR_XML_DECODING:
    return "failed: xml-decoding-error"
  else:
    assert False

def push_result(filename, code, status, output, error=None):
  result = {
    "filename": filename,
    "code": code,
    "status": status,
    "output": output,
    "error": error
  }
  results[status].append(result)

def output_result(result):
  print result['filename']
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
  num_of_test = 0
  success = 0
  for status in xrange(len(results)):
    items = results[status]
    if len(items) == 0:
      continue
    print status_name(status)
    for re in items:
      output_result(re)
      num_of_test += 1
      if re['status'] == STATUS_SUCCESS:
        success += 1
    print ''
  print 'success: ' + str(success) + ' tests'
  print 'failed: ' + str(num_of_test - success) + ' tests'

def run_test(filename):
  source = test_path + '/' + filename
  file = open(source, 'r')
  code = file.read()
  code = code.replace('\n', '')

  try:
    re = subprocess.check_output(["./block_of_ocaml", code],
        stderr=subprocess.STDOUT)
    try:
      xml_root = ElementTree.fromstring(re)
      push_result(filename, code, STATUS_SUCCESS, xml_root)
    except ElementTree.ParseError as e:
      push_result(filename, code, STATUS_ERROR_XML_PARSING, re, e)
  except subprocess.CalledProcessError as e:
      push_result(filename, code, STATUS_ERROR_XML_DECODING, None, e)

def run_all_tests():
  test_files = [f for f in listdir(test_path) if isfile(join(test_path, f))]
  for filename in test_files:
    run_test(filename)

run_all_tests()
output_all_results()
