ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      withCredentials([string(credentialsId: 'connectapi-connect-license-key', variable: 'license')]) {
          sh "RSC_LICENSE=$license make test"
      }
      print "Finished integration tests"
    }
  }
}
