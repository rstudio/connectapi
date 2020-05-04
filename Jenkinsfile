ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      withCredentials([string(credentialsId: 'connectapi-connect-license-key', variable: 'license')]) {
          sh "RSC_LICENSE=$license make test"
          
          // Outputs
          archiveArtifacts artifacts: "test-results-*", fingerprint: true, allowEmptyArchive: true
          archiveArtifacts artifacts: "integrated-results-*", fingerprint: true, allowEmptyArchive: false
          junit "*.xml"
      }
      print "Finished integration tests"
    }
  }
}
