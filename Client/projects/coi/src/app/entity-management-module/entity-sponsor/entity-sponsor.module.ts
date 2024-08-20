import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import { EntitySponsorComponent } from './entity-sponsor.component';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../shared/shared.module';
import { MatIconModule } from '@angular/material/icon';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';
import { EntityOverviewService } from '../entity-overview/entity-overview.service';
import { EntitySponsorDetailsComponent } from './entity-sponsor-details/entity-sponsor-details.component';
import { EntitySponsorRiskComponent } from './entity-sponsor-risk/entity-sponsor-risk.component';
import { EntitySponsorAttachmentsComponent } from './entity-sponsor-attachments/entity-sponsor-attachments.component';

@NgModule({
    declarations: [
        EntitySponsorComponent,
        EntitySponsorDetailsComponent,
        EntitySponsorRiskComponent,
        EntitySponsorAttachmentsComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild([{ path: '', component: EntitySponsorComponent }]),
        FormsModule,
        SharedModule,
        MatIconModule,
        SharedComponentModule,
        SharedEntityManagementModule
    ],
    providers: [
        EntityOverviewService
    ]
})
export class EntitySponsorModule { }
