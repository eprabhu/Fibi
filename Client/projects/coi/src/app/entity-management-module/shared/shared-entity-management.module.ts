import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SharedModule } from '../../shared/shared.module';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { EntityCommonCardComponent } from './entity-common-card/entity-common-card.component';
import { EntityAttachmentModalComponent } from './entity-attachment-modal/entity-attachment-modal.component';
import { EntityRiskSectionComponent } from './entity-risk-section/entity-risk-section.component';
import { FormsModule } from '@angular/forms';

@NgModule({
    declarations: [
        EntityCommonCardComponent,
        EntityRiskSectionComponent,
        EntityAttachmentModalComponent,
    ],
    imports: [
        FormsModule,
        CommonModule,
        SharedModule,
        SharedComponentModule,
    ],
    exports: [
        EntityCommonCardComponent,
        EntityRiskSectionComponent,
        EntityAttachmentModalComponent,
    ]
})
export class SharedEntityManagementModule { }
