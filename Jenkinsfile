environment {
  RSC_LICENSE = "test" // credentials('connectapi-connect-license-key')
}

ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      sh "env"
      // sh "echo ${RSC_LICENSE}"
      sh "make test"
      print "Finished integration tests"
    }
  }
}
