ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      withCredentials([string(credentialsId: 'connectapi-connect-license-key', variable: 'license')]) {
        try{
          print "====> Running tests"
          sh "RSC_LICENSE=$license make test"
        } catch(err) {
          print "${err}"
        } finally {
          print "====> Cleanup environment"
          sh "make clean"
        }
          
        // Outputs
        archiveArtifacts artifacts: "test-results-*", fingerprint: true, allowEmptyArchive: true
        archiveArtifacts artifacts: "integrated-results-*", fingerprint: true, allowEmptyArchive: false
        junit "*.xml"
      }
      print "Finished integration tests"
    }
  }
}
