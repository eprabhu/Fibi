import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityRelationshipComponent } from './entity-relationship.component';
import { RouterModule, Routes } from '@angular/router';
import { ProjectListComponent } from './project-list/project-list.component';
import { EntityListComponent } from './entity-list/entity-list.component';
import { FormsModule } from '@angular/forms';
import { IncompleteWarningComponent } from './incomplete-warning/incomplete-warning.component';
import {SharedModule} from "../../../shared/shared.module";


const routes: Routes = [{path: '', component: EntityRelationshipComponent}];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    SharedModule,
    FormsModule
  ],
  declarations: [
    EntityRelationshipComponent,
    ProjectListComponent,
    EntityListComponent,
    IncompleteWarningComponent
  ]
})
export class EntityRelationshipModule { }
