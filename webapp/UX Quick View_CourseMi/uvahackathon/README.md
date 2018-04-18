# Web Application Problem Definition

## Challenge:

Medical students have a lot of material to read/explore, a lot of courses to attend, and very little time to spare. As a result, a poorly designed user experience, combined will a large amount of applications to use, can result in valuable time lost, and a lot of frustration.

The challenge is to build a mobile-ready experience for students to navigate all of their applications and courses with as few clicks as possible, and without the students getting lost in the process. You can use any technology you choose, but you should explain why you chose that particular technology.

### Things to Consider:

* Students in different medical years could have access to different applications and courses.
* Students rely a lot on their calendars
* Each course has its own page with potentially 1-10 "weeks," each containing multiple sessions (e.g., lectures, labs). Each session could have its own page with details, as well as materials to download.
* Students accumulate a lot of quantitative and qualitative data over the full four years that is seldom available to them for review purposes

####Teammates for CourseMi: 

Travis Gray 

Emmanuel Ogbewe

Josue Sossou


#### Application Solution Frontend Design:

We created a one page Bootstrap  mobile ready design that allows for Medical Students to quickly see vital information. This vital student information is often hidden behind alot of clicks and menus in other learning management applications. 

We used Bootstrap front-end Framework to accomplish our one page design. 

Mobile devices continue to grow in popularity year after year. Cisco predicts that global mobile data traffic will increase nearly 11-fold between 2013 and 2018. The need to have a responsive website is becoming increasingly important.

Creating mobile ready websites is a great with Bootstrap thanks to the fluid grid layout that dynamically adjusts to the proper screen resolution. 

Using Bootstraps classes and grid system  we created different modules for different students apps (Suchs as Onenote or Google Calendar) and responsive navbar that uses javascript to take users to desired content and information on the page.
 
 ### Improvements and Backend Design Considerations:
 
 Although we werent able to complete the backend of this Application we would expand functionality of our design using Spring Boot and Spring Security with Thymeleaf to hide views based on access for Admin (Teacher and
 Student). 
 
 Since these modules on our homepage would be dynmaic passing in values from our Java Backend using Spring Boot each student would be left with a unique and modular experince this design approach could scale well for a large number of students within a university. 
 
 More information Here: https://memorynotfound.com/spring-boot-spring-security-thymeleaf-form-login-example/