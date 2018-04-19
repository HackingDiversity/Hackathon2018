# Data Science Problem Definition

## Requirements

We ask that you either use [Jupyter Notebooks](https://jupyter.org) or [Azure Notebooks](https://notebooks.azure.com) for this problem.

> Azure Notebooks supports R, Python, and F# as programming languages.

## Challenge

Medical education is different from other education in that it has a limited number of schools and enrollments, but requires large amounts of data and meta data for evaluations and longitudinal studies. The small number of schools means that medical education is often a "tacked on" feature of educational software, since the audience is limited and the revenue is small. Most medical education solutions are homegrown, which can leave data collection and evaluation lacking. In some situations, a large amount of disparate meta data is connected, but never cleaned or evaluated, so it often can be riddled with bad data, loose correlations, and no outlet for review.

Given the linked data set--which is a small subset of the actual data--the challenge is to find meaningful correlations between data points, while avoiding getting trapped by bad data. What techniques would you use to clean the data set? Which meta data attributes are more important than others in predicting future relationships and performance? How can this be visually represented most appropriately to help decision-makers?

Consider this a "Wild West" challenge. What--if anything--can be inferred from the data?

### Things to Consider

* The root table for the data pull is an "Object times Attribute" table that links items (e.g., question data, evaluation data) with attributes on that object (e.g., learning objectives, category threads). This data is about relationships.
* The resulting CSV file is a combination of several different database tables. The root table is just where relationships across data points are stored.
* There are different object types, so not all rows/records are related to each other.
* Different explorations will require different data cleaning. Some columns might be unnecessary in your specific exploration.
* If you cannot open/process the data set on your computer, you can use a smaller version of the data set, or you can use [Azure Notebooks](https://notebooks.azure.com) to process the file.
* Errors have been purposely introduced into the data/file.
* Be creative, if you can. Don't go with the obvious analysis.

### Data Dictionary

* Object - A primary item stored in the database. This could be a question that is used on an exam, an exam itself, an evaluation/assesment, etc. (data in the Object column is an ID of QuestionID, ExamID, AssessmentID, etc.).
* Attribute - Value of the meta data that is being stored to label/describe the object.
* Type - Name of the object or name for the attribute.
* DateStart - When the attribute was first related to the object.
* DateEnd - When the attribute was disassociated with the object.
* Source - Which database the object is stored in.
* Assessment - An evaluation/assessment of skills not from an exam.
* Count - A-J represent the letter designation of an exam question answer. The count is the total number of student responses for that designation.

> Disclaimer: Although the data is based on real relationships, exam data is *not* in order to protect the integrity of the exams.
