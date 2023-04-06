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
import { ConflictManagementSummaryComponent } from './review/conflict-management-summary/conflict-management-summary.component';
import {MatIconModule} from "@angular/material/icon";
import { SharedComponentModule } from '../../../../../fibi/src/app/shared-component/shared-component.module';
import { MatInputModule } from '@angular/material/input';
import { MatFormFieldModule } from '@angular/material/form-field';
import { AddCommentSliderComponent } from './review/relationship-summary/add-comment-slider/add-comment-slider.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';

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
        SharedModule
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
        AddCommentSliderComponent
    ],
    providers: [
        CoiSummaryEventsAndStoreService,
        CoiSummaryService
    ]
})
export class SummaryModule {
}
