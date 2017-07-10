Feature: Test Mocha Project

  Scenario: Testing a project
    Given I visit sample project file "sample-project/test/parse-float-test.js"
    When I switch to buffer "parse-float-test.js"
    Then I should be in buffer "parse-float-test.js"
    When I run the command "mocha-test-project"
    And I wait for the compilation to finish
    Then I should see buffer "*mocha tests*"
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
    Given I visit sample project file "sample-project/src/parse-float.js"
    When I switch to buffer "parse-float.js"
    Then I should be in buffer "parse-float.js"
    When I run the command "mocha-test-project"
    And I wait for the compilation to finish
    Then I should see buffer "*mocha tests*"
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

  Scenario: Testing a single test file
    Given I visit sample project file "sample-project/test/parse-float-test.js"
    When I switch to buffer "parse-float-test.js"
    Then I should be in buffer "parse-float-test.js"
    When I go to line "5"
    When I run the command "mocha-test-file"
    And I wait for the compilation to finish
    Then I should see buffer "*mocha tests*"
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha started
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 passing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha finished
      """

  Scenario: Testing a particular line in a test file
    Given I visit sample project file "sample-project/test/parse-int-test.js"
    When I switch to buffer "parse-int-test.js"
    Then I should be in buffer "parse-int-test.js"
    When I switch to js2-mode
    And I go to line "5"
    And I run the command "mocha-test-at-point"
    And I wait for the compilation to finish
    Then I should see buffer "*mocha tests*"
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha started
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 passing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 failing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha exited abnormally
      """
    When I go to line "7"
    And I run the command "mocha-test-at-point"
    And I wait for the compilation to finish
    Then I should see buffer "*mocha tests*"
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha started
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 passing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha finished
      """
    When I go to line "13"
    And I run the command "mocha-test-at-point"
    And I wait for the compilation to finish
    Then I should see buffer "*mocha tests*"
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha started
      """
    And I should see contents in buffer "*mocha tests*":
      """
      1 failing
      """
    And I should see contents in buffer "*mocha tests*":
      """
      Mocha exited abnormally
      """

  Scenario: Testing a particular line in a test file without js2-mode
    Given I visit sample project file "sample-project/test/parse-int-test.js"
    When I switch to buffer "parse-int-test.js"
    Then I should be in buffer "parse-int-test.js"
    And I go to line "7"
    Then I run the command "mocha-test-at-point" expecting an error
