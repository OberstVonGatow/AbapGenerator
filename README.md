# AbapGenerator
Generate ABAP Objects from Templates

Use-Case:
If you use a Design Pattern like Model-View-Controller, you always have to create a lot of Object for a new Project.
You create a Package, a Report, a Model-Class, a View-Class and maybe other Classes.
There is also the hassle to pass the Selection Screen Data from the Report to the Model Class.
There are generic ways to read the Selection Data of the Report but they always made it more complex, difficult to read and error-prone. 

With ABAP Generator:
- You create your Template Packages with your needed Classes and Report once
- When you want to start a new Project you just start Transaction ZABAP_GENERATOR with your Template
- Initialization (in ZABAP_GENERATOR)
  - The Template Objects will be copied and changed accordingly (Templatename will be replaced with the Projectname)  
- Then you change the Selection Screen of your just generated Report as you like in ADT or SE38
- Update Selection Screen (in ZABAP_GENERATOR)
  - Your Report Selection Screen will be read and the Parameters/Select-Options passed to the Model Class
  - Model Class will be changed to fit the Selection Data
  - If Dict. Range Tables is checked, TableTypes with Rangetables will be created if not already created.

You can change the Default Values and Settings in Table ZZT_ABAP_GEN with SM30.
(Default Values will be generated in ZZT_ABAP_GEN with first run of ZABAP_GENERATOR).

Transaction ZABAP_GENERATOR Initialization:

![transaction init](docs/img/transaction_init.png)

Template Package:

![template_package](docs/img/template_package.png)

New Package:

![demo_package](docs/img/demo_package.png)

Update Report as you like:

![edit_report](docs/img/edit_report_selection.png)

Transaction ZABAP_GENERATOR Update:

![transaction upd](docs/img/transaction_upd.png)

Updated Report:

![updated_report](docs/img/updated_report.png)

Updated Model:

![updated_model](docs/img/updated_model.png)
