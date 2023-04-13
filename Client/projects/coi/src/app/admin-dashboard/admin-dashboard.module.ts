import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {AdminDashboardComponent} from './admin-dashboard.component';
import {RouterModule, Routes} from "@angular/router";
import { SharedModule } from '../shared/shared.module';
import { MatIconModule } from '@angular/material/icon';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { CoiSharedModule } from '../disclosure/shared/shared.module';
import { AdminDashboardService } from './admin-dashboard.service';
import { FormsModule } from '@angular/forms';

const routes: Routes = [{path: '', component: AdminDashboardComponent}];

@NgModule({
    declarations: [
        AdminDashboardComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        SharedModule,
        CoiSharedModule,
        SharedComponentModule,
        MatIconModule,
        FormsModule,
        SharedModule
    ],
    providers: [AdminDashboardService]
})
export class AdminDashboardModule {
}
