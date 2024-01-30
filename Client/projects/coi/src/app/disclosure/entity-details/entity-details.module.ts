import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityDetailsComponent } from './entity-details.component';
import { RouterModule, Routes } from '@angular/router';
import { MatIconModule } from '@angular/material/icon';
import { SharedModule } from '../../shared/shared.module';
import { EntityQuestionnaireComponent } from './entity-questionnaire/entity-questionnaire.component';
import { EntityDetailsService } from './entity-details.service';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { ViewRelationshipDetailsComponent } from './view-relationship-details/view-relationship-details.component';
import { EntityDetailsRouteGuardService } from './entity-details-route-guard.service';
import { SfiHistoryComponent } from './sfi-history/sfi-history.component';

const routes: Routes = [{ path: 'entity', component:EntityDetailsComponent, canDeactivate: [EntityDetailsRouteGuardService] }]

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    MatIconModule,
    SharedModule,
    FormsModule,
    SharedComponentModule,
  ],
  declarations: [
    EntityDetailsComponent,
    EntityQuestionnaireComponent,
    ViewRelationshipDetailsComponent,
    SfiHistoryComponent
  ],
  providers:[
    EntityDetailsService,
    EntityDetailsRouteGuardService
    ], 
  exports : [EntityDetailsComponent]
})
export class EntityDetailsModule { }
