Feature: Run Mocha Tests

  Scenario: Testing a project
    Given I visit sample project file "test/parse-float-test.js"
    Then I should see buffer "parse-float-test.js"
    When I run the command "mocha-test-project"
    And I should see buffer "*mocha tests*"
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha started
      """
    And I should see contents in buffer "*mocha tests*":
      """
      2 passing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 failing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha exited abnormally
      """

  Scenario: Testing a project from a source file
    Given I visit sample project file "src/parse-float.js"
    Then I should see buffer "parse-float.js"
    When I run the command "mocha-test-project"
    And I should see buffer "*mocha tests*"
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha started
      """
    And I should see contents in buffer "*mocha tests*":
      """
      2 passing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 failing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha exited abnormally
      """
