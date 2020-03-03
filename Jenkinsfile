ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      withCredentials([string(credentialsId: 'connectapi-connect-license-key', variable: 'license')]) {
          sh "RSC_LICENSE=$license make test"
          
          // Outputs
          archiveArtifacts artifacts: "test-results-*", fingerprint: true, allowEmptyArchive: true
          junit "*.xml"
      }
      print "Finished integration tests"
    }
  }
}
