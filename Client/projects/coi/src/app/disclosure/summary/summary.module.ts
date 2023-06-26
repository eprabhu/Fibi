import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterModule} from '@angular/router';

import {SummaryComponent} from './summary.component';
import {ReviewComponent} from './review/review.component';
import {ToolKitComponent} from './tool-kit/tool-kit.component';
import {CoiSummaryEventsAndStoreService} from './coi-summary-events-and-store.service';
import {CertifySummaryComponent} from './review/certify-summary/certify-summary.component';
import {RelationshipSummaryComponent} from './review/relationship-summary/relationship-summary.component';
import {ScreeningQuestionnaireSummaryComponent} from './review/screening-questionnaire-summary/screening-questionnaire-summary.component';
import {SfiSummaryComponent} from './review/sfi-summary/sfi-summary.component';
import {CoiSummaryService} from './coi-summary.service';
import {FormsModule} from '@angular/forms';
import {SharedModule} from "../../shared/shared.module";
import {ConflictManagementSummaryComponent} from './review/conflict-management-summary/conflict-management-summary.component';
import {MatIconModule} from "@angular/material/icon";
import {MatInputModule} from '@angular/material/input';
import {MatFormFieldModule} from '@angular/material/form-field';
import {SharedComponentModule} from "../../shared-components/shared-component.module";
import { AddConflictSliderComponent } from './review/relationship-summary/add-conflict-slider/add-conflict-slider.component';

@NgModule({
    imports: [
        CommonModule,
        FormsModule,
        RouterModule.forChild([{path: '', component: SummaryComponent}]),
        SharedModule,
        MatIconModule,
        MatInputModule,
        MatFormFieldModule,
        SharedComponentModule,
    ],
    declarations: [
        SummaryComponent,
        ToolKitComponent,
        ReviewComponent,
        SfiSummaryComponent,
        ScreeningQuestionnaireSummaryComponent,
        RelationshipSummaryComponent,
        CertifySummaryComponent,
        ConflictManagementSummaryComponent,
        AddConflictSliderComponent
    ],
    providers: [
        CoiSummaryEventsAndStoreService,
        CoiSummaryService
    ]
})
export class SummaryModule {
}
