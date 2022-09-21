import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TemplateListComponent } from './template-list/template-list.component';
import { TemplateComponent } from './template/template.component';
import { RouterModule } from '@angular/router';

const routes = [{path: '', component: TemplateListComponent},
   { path: 'template-list', component: TemplateListComponent },
    { path: 'template', component: TemplateComponent }];

@NgModule({
  imports: [RouterModule.forChild(routes),
    CommonModule
  ],
  declarations: [],
  exports: [RouterModule]
})
export class ReportsRoutingModule { }
