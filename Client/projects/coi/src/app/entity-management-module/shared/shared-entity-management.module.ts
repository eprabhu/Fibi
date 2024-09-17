import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '../../shared/shared.module';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { EntityCommonCardComponent } from './entity-common-card/entity-common-card.component';
import { EntityRiskSectionComponent } from './entity-risk-section/entity-risk-section.component';
import { FormsModule } from '@angular/forms';
import { EntityAttachmentSectionModule } from './entity-attachment-section/entity-attachment-section.module';
import { DuplicateEntityCheckComponent } from './duplicate-entity-check/duplicate-entity-check.component';

@NgModule({
    declarations: [
        EntityCommonCardComponent,
        EntityRiskSectionComponent,
        DuplicateEntityCheckComponent,
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
    ]
})
export class SharedEntityManagementModule { }
