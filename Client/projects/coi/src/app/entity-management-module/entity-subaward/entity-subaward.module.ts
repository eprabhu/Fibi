import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntitySubawardComponent } from './entity-subaward.component';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../shared/shared.module';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { AddAttachmentModalModule } from '../../common/header/add-attachment-modal/add-attachment-modal.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';
import { EntityOverviewService } from '../entity-overview/entity-overview.service';
import { EntitySubawardDetailsComponent } from './entity-subaward-details/entity-subaward-details.component';
import { EntitySubawardRiskComponent } from './entity-subaward-risk/entity-subaward-risk.component';
import { EntitySubawardAttachmentsComponent } from './entity-subaward-attachments/entity-subaward-attachments.component';



@NgModule({
    declarations: [
        EntitySubawardComponent,
        EntitySubawardDetailsComponent,
        EntitySubawardRiskComponent,
        EntitySubawardAttachmentsComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild([{ path: '', component: EntitySubawardComponent }]),
        FormsModule,
        SharedModule,
        SharedComponentModule,
        AddAttachmentModalModule,
        SharedEntityManagementModule
    ],
    providers: []
})
export class EntitySubawardModule { }
