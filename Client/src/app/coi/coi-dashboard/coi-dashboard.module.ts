import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {RouterModule} from '@angular/router';

import {CoiDashboardComponent} from './coi-dashboard.component';
import {ActionListComponent} from './action-list/action-list.component';
import {FyiComponent} from './fyi/fyi.component';
import {QuickLinksComponent} from './quick-links/quick-links.component';
import {CoiDashboardService} from './coi-dashboard.service';
import {SharedModule} from '../../shared/shared.module';
import {CoiSharedModule} from '../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { DataStoreService } from '../services/data-store.service';
import { CoiService } from '../services/coi.service';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        CoiSharedModule,
        FormsModule,
        RouterModule.forChild([{path: '', component: CoiDashboardComponent}])
    ],
    declarations: [
        CoiDashboardComponent,
        QuickLinksComponent,
        FyiComponent,
        ActionListComponent,
    ],
    providers: [
        CoiDashboardService,
        DataStoreService,
        CoiService
    ]
})
export class CoiDashboardModule {
}
