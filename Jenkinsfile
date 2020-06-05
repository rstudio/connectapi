isMasterBranch  = (env.BRANCH_NAME == 'master')

messagePrefix = "Pipeline build <${env.BUILD_URL}|${env.BUILD_DISPLAY_NAME}> ${branchDescription}"

slackChannelFail = "#sol-eng-bots"

ansiColor('xterm') {
  stage('integration_test') {
    node('docker') {
      checkout scm
      print "Running integration tests"
      withCredentials([string(credentialsId: 'connectapi-connect-license-key', variable: 'license')]) {
        try{
          print "====> Running tests"
          gitSHA = sh(returnStdout: true, script: 'git rev-parse HEAD').trim()
          shortSHA = gitSHA.take(6)

          // Update our Slack message metadata with commit info once available.
          messagePrefix = messagePrefix + " of <https://github.com/rstudio/connectapi/commit/${gitSHA}|${shortSHA}>"

          sh "RSC_LICENSE=$license make test"
        } catch(err) {
          message = "${messagePrefix} failed: ${err}"
          print "${message}"
          if (isMasterBranch) {
            slackSend channel: slackChannelFail, color: 'bad', message: message
          }
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
