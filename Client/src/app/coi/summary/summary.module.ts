import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';

import { SummaryComponent } from './summary.component';
import { ReviewComponent } from './review/review.component';
import { ToolKitComponent } from './tool-kit/tool-kit.component';
import { SharedModule } from '../../shared/shared.module';
import { CoiSummaryEventsAndStoreService } from './coi-summary-events-and-store.service';
import { CertifySummaryComponent } from './review/certify-summary/certify-summary.component';
import { RelationshipSummaryComponent } from './review/relationship-summary/relationship-summary.component';
import { ScreeningQuestionnaireSummaryComponent } from './review/screening-questionnaire-summary/screening-questionnaire-summary.component';
import { SfiSummaryComponent } from './review/sfi-summary/sfi-summary.component';
import { CoiSummaryService } from './coi-summary.service';
import { FormsModule } from '@angular/forms';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild([{ path: '', component: SummaryComponent }]),
    ],
    declarations: [
        SummaryComponent,
        ToolKitComponent,
        ReviewComponent,
        SfiSummaryComponent,
        ScreeningQuestionnaireSummaryComponent,
        RelationshipSummaryComponent,
        CertifySummaryComponent
    ],
    providers: [
        CoiSummaryEventsAndStoreService,
        CoiSummaryService
    ]
})
export class SummaryModule { }
