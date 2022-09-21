import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {CoiAdminDashboardComponent} from './coi-admin-dashboard.component';
import {SharedModule} from "../../shared/shared.module";
import {RouterModule} from "@angular/router";
import {CoiAdminDashboardService} from "./coi-admin-dashboard.service";
import {FormsModule} from '@angular/forms';
import {CoiSharedModule} from "../shared/shared.module";
import { DataStoreService } from '../services/data-store.service';


@NgModule({
    declarations: [CoiAdminDashboardComponent],
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        CoiSharedModule,
        RouterModule.forChild([{path: '', component: CoiAdminDashboardComponent}])
    ],
    providers: [CoiAdminDashboardService, DataStoreService]
})
export class CoiAdminDashboardModule {
}
