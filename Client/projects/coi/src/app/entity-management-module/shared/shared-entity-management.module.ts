import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '../../shared/shared.module';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { EntityCommonCardComponent } from './entity-common-card/entity-common-card.component';
import { EntityRiskSectionComponent } from './entity-risk-section/entity-risk-section.component';
import { FormsModule } from '@angular/forms';
import { EntityAttachmentSectionModule } from './entity-attachment-section/entity-attachment-section.module';
import { DuplicateEntityCheckComponent } from './duplicate-entity-check/duplicate-entity-check.component';
import { DuplicateMarkingConfirmationComponent } from './duplicate-marking-confirmation/duplicate-marking-confirmation.component';
import { EntityDetailsPopupCardComponent } from './entity-details-popup-card/entity-details-popup-card.component';
import { RiskEditHistorySlider } from './entity-risk-section/risk-edit-history-slider/risk-edit-history-slider.component';
import { EntityCommonQuestionnaireComponent } from './entity-common-questionnaire/entity-common-questionnaire.component';

@NgModule({
    declarations: [
        EntityCommonCardComponent,
        EntityRiskSectionComponent,
        DuplicateEntityCheckComponent,
        DuplicateMarkingConfirmationComponent,
        EntityDetailsPopupCardComponent,
        RiskEditHistorySlider,
        EntityCommonQuestionnaireComponent
    ],
    imports: [
        FormsModule,
        CommonModule,
        SharedModule,
        SharedComponentModule,
        EntityAttachmentSectionModule
    ],
    exports: [
        EntityCommonCardComponent,
        EntityRiskSectionComponent,
        EntityAttachmentSectionModule,
        DuplicateEntityCheckComponent,
        EntityDetailsPopupCardComponent,
        DuplicateMarkingConfirmationComponent,
        RiskEditHistorySlider,
        EntityCommonQuestionnaireComponent
    ]
})
export class SharedEntityManagementModule { }
