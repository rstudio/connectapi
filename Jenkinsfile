ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      make test
      print "Finished integration tests"
    }
  }
}
