import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityDetailsComponent } from './entity-details.component';
import { RouterModule, Routes } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { AddRelationshipComponent } from './add-relationship/add-relationship.component';
import { AddEntityDetailsComponent } from './add-entity-details/add-entity-details.component';
import { SharedModule } from '../../shared/shared.module';
import { EntityQuestionnaireComponent } from './entity-questionnaire/entity-questionnaire.component';
import { EntityDetailsService } from './entity-details.service';
import { EntityDetailsGuardService } from './entity-details-guard.service';
import { FormsModule } from '@angular/forms';

const routes: Routes = [{ path: '', component:EntityDetailsComponent, canActivate:[EntityDetailsGuardService] }]

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    MatIconModule,
    SharedModule,
    FormsModule
  ],
  declarations: [
    EntityDetailsComponent,
    AddEntityDetailsComponent,
    AddRelationshipComponent,
    EntityQuestionnaireComponent

  ],
  providers:[EntityDetailsService,EntityDetailsGuardService]
})
export class EntityDetailsModule { }
