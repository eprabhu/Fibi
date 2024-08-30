import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityComplianceComponent } from './entity-compliance.component';
import { RouterModule } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../../shared/shared.module';
import { MatIconModule } from '@angular/material/icon';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedEntityManagementModule } from '../shared/shared-entity-management.module';

@NgModule({
    declarations: [
        EntityComplianceComponent,
    ],
    imports: [
        CommonModule,
        RouterModule.forChild([{ path: '', component: EntityComplianceComponent }]),
        FormsModule,
        SharedModule,
        MatIconModule,
        SharedComponentModule,
        SharedEntityManagementModule
    ],
    providers: []
})
export class EntityComplianceModule {}
