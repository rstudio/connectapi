isMasterBranch  = (env.BRANCH_NAME == 'master')

if (env.CHANGE_TITLE == null) {
    // When running against a branch, BRANCH_NAME is the branch name and CHANGE_TITLE is null.
    branchDescription = "on <${env.JOB_URL}|${env.BRANCH_NAME}>"
} else {
    // When running against a PR, BRANCH_NAME is PR-XXXX and CHANGE_TITLE is the PR name.
    // There is no access to the branch name.
    branchDescription = "for <${env.JOB_URL}|${env.BRANCH_NAME}>: <${env.CHANGE_URL}|${env.CHANGE_TITLE}>"
}
messagePrefix = "connectapi pipeline build <${env.BUILD_URL}|${env.BUILD_DISPLAY_NAME}> ${branchDescription}"

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
          sh "ls -ltR"
        }

        // Outputs
        archiveArtifacts artifacts: "test-results-*", fingerprint: true, allowEmptyArchive: true
        archiveArtifacts artifacts: "tests/integrated/integrated-results-*", fingerprint: true, allowEmptyArchive: false
        junit "tests/integrated/*.xml"
      }
      print "Finished integration tests"
    }
  }
}
