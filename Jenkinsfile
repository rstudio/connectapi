ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      withCredentials([string(credentialsId: 'connectapi-connect-license-key', variable: 'license')]) {
          environment {
            RSC_LICENSE = license
          }
          sh "make test"
      }
      print "Finished integration tests"
    }
  }
}
