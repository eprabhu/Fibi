import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { FormsModule } from '@angular/forms';

import { CoiComponent } from './coi.component';
import { CoiRoutes } from './coi.routing';
import { SharedModule } from '../shared/shared.module';
import { ResolveServiceService } from './services/resolve-service.service';
import { RouterGuardService } from './services/router-guard.service';
import { CoiService } from './services/coi.service';
import { DataStoreService } from './services/data-store.service';
import { CoiSharedModule } from './shared/shared.module';
import { ActivityComponent } from './activity-track/activity.component';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        CoiRoutes,
        FormsModule,
        CoiSharedModule
    ],
    declarations: [
        CoiComponent,
        ActivityComponent
    ],
    providers: [
        ResolveServiceService,
        DataStoreService,
        CoiService,
        RouterGuardService
    ]
})
export class CoiModule { }
