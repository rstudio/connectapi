ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      sh "make test"
      print "Finished integration tests"
    }
  }
}
