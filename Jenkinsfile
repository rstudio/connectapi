ansiColor('xterm') {
  stage('integration_test') {
    environment {
      RSC_LICENSE = credentials('connectapi-connect-license-key')
    }
    node('docker') {
      checkout scm
      print "Running integration tests"
      sh "make test"
      print "Finished integration tests"
    }
  }
}
