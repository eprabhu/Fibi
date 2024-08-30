import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { EntityManagementModuleComponent } from './entity-management-module.component';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedModule } from '../shared/shared.module';
import { MatIconModule } from '@angular/material/icon';
import { HeaderDetailsComponent } from './header-details/header-details.component';
import { RightPanelComponent } from './right-panel/right-panel.component';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { SharedEntityManagementModule } from './shared/shared-entity-management.module';
import { EntityDataStoreService } from './entity-data-store.service';
import { EntityConfigurationResolveGuardService, EntityManagementResolveService, EntityPathResolveService } from './entity-management-resolve.service';
import { EntityManagementService } from './entity-management.service';

const routes: Routes = [
    {
        path: '', component: EntityManagementModuleComponent, canActivate: [EntityManagementResolveService],
        children: [
            { path: '', redirectTo: 'entity-overview', pathMatch: 'full' },
            { path: 'entity-overview', loadChildren: () => import('./entity-overview/entity-overview.module').then(m => m.EntityFormOverviewModule), canDeactivate: [EntityPathResolveService] },
            { path: 'entity-sponsor', loadChildren: () => import('./entity-sponsor/entity-sponsor.module').then(m => m.EntitySponsorModule), canDeactivate: [EntityPathResolveService] },
            { path: 'entity-subaward', loadChildren: () => import('./entity-subaward/entity-subaward.module').then(m => m.EntitySubawardModule), canDeactivate: [EntityPathResolveService] },
            { path: 'entity-compliance', loadChildren: () => import('./entity-compliance/entity-compliance.module').then(m => m.EntityComplianceModule), canDeactivate: [EntityPathResolveService] },
            { path: 'entity-notes', loadChildren: () => import('./entity-notes/entity-notes.module').then(m => m.EntityNotesModule) },
            { path: 'entity-attachments', loadChildren: () => import('./entity-attachment/entity-attachment.module').then(m => m.EntityAttachmentModule) },
            { path: 'entity-history', loadChildren: () => import('./entity-history/entity-history.module').then(m => m.EntityHistoryModule) },
        ]
    },
    {
        path: 'create-entity', loadComponent: () => import('./create-entity/create-entity.component').then(c => c.CreateEntityComponent),
        resolve: { entityConfig: EntityConfigurationResolveGuardService }
    }
];

@NgModule({
    declarations: [
        EntityManagementModuleComponent,
        HeaderDetailsComponent,
        RightPanelComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        FormsModule,
        SharedModule,
        MatIconModule,
        SharedComponentModule,
        SharedEntityManagementModule
    ],
    providers: [
        EntityDataStoreService,
        EntityManagementResolveService,
        EntityConfigurationResolveGuardService,
        EntityManagementService,
        EntityPathResolveService
    ]
})
export class EntityManagementModuleModule {}
