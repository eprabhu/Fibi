import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DefineRelationshipComponent } from './define-relationship.component';
import { FormsModule } from '@angular/forms';
import { Routes, RouterModule } from '@angular/router';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedModule } from '../../shared/shared.module';
import { ProjectSfiNavigationComponent } from './project-sfi-navigation/project-sfi-navigation.component';
import { ProjectSfiRelationshipComponent } from './project-sfi-relationship/project-sfi-relationship.component';
import { ProjectDetailsCardComponent } from './project-details-card/project-details-card.component';
import { CoiSummaryService } from '../summary/coi-summary.service';
import { ProjectSfiConflictComponent } from './project-sfi-conflict/project-sfi-conflict.component';
import { DefineRelationshipService } from './services/define-relationship.service';

const routes: Routes = [{ path: '', component: DefineRelationshipComponent }];

@NgModule({
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        FormsModule,
        SharedModule,
        SharedComponentModule,
    ],
    declarations: [
        DefineRelationshipComponent,
        ProjectSfiRelationshipComponent,
        ProjectDetailsCardComponent,
        ProjectSfiNavigationComponent,
        ProjectSfiConflictComponent,
    ],
    exports: [
        ProjectSfiRelationshipComponent,
        ProjectSfiNavigationComponent,
    ]
})
export class DefineRelationshipModule { }
