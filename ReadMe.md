# EasyWeb - Bootstrap framework for Delphi


EasyWeb is basically a project I've been working on in my spare time which allows Delphi developers to write Bootstrap web applications.

Is it useful?  I'm not really sure, It's certainly not finished, but it would be good to hear what you think. I'll continue to develop it as and and when time allows.

By all means get involved if you'd like to help :-)

##Features

*  Generates the HTML, CSS and JS require to make the development feel as "Delphi-like" as possible.
*  Ajax updating of web pages
*  Simple form stack push/pop system


##Installation

I've created packages for XE8 and Delphi 10.3 (as these are the 2 environments I have installed) but it should be easy to install on anything inbetween.

To install, open up the EasyWeb_XX.groupproj from the source directory which corrasponds to your Delphi version.
Right-click on the Project Group Root node in the Project Manager and select "Build All".
Then right-click on then EasyWebDsgn_XX (design-time) package and click on "Install".
You'll also want to add the path of the EasyWeb source folder to your Delphi library path..

You should then see a new "EasyWeb" category in your Delphi File->New options alond with a new EasyWeb category on your Object Inspector.

## Support the project

If you find the components useful feel free to donate and one day I might just get that Tesla ;-)

[![Donate](https://img.shields.io/badge/Donate-PayPal-green.svg)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=WKYL4XUEY46J2)


##The "To-Do" List

*  Add more demos
*  Add more components
*  Extend existing component to support more properties/events
*  Add unit tests
*  Convert to async http requests
*  Create component icons
*  Probably fix loads of bugs which I haven't found

If anyone feels they want to get involved then feel free ;-)

##Components included

These components wrap around the standard HTML/Bootstrap components.

*  TEWButton
*  TEWDropDown
*  TEWButtonGroup
*  TEWEdit
*  TEWComboBox
*  TEWMemo
*  TEWProgressBar
*  TEWCheckBox
*  TEWLabel
*  TEWImage
*  TEWLayout
*  TEWLayoutGrid
*  TEWTimer
*  TEWNavBar
*  TEWDialog
*  TEWCheckBoxGroup
*  TEWRadioGroup
*  TEWTable

