import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityComplianceComponent } from './entity-compliance.component';
import { RouterModule } from '@angular/router';
import { EntityComplianceRiskComponent } from './entity-compliance-risk/entity-compliance-risk.component';
import { EntityComplianceAttachmentsComponent } from './entity-compliance-attachments/entity-compliance-attachments.component';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../shared/shared.module';
import { MatIconModule } from '@angular/material/icon';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { AddAttachmentModalModule } from '../../common/header/add-attachment-modal/add-attachment-modal.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';

@NgModule({
  declarations: [
    EntityComplianceComponent,
    EntityComplianceRiskComponent,
    EntityComplianceAttachmentsComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: EntityComplianceComponent}]),
    FormsModule,
    SharedModule,
    MatIconModule,
    SharedComponentModule,
    AddAttachmentModalModule,
    SharedEntityManagementModule
  ],
  providers: []
})
export class EntityComplianceModule { }
