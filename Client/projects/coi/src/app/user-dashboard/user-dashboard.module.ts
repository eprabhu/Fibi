import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {UserDashboardComponent} from './user-dashboard.component';
import {RouterModule, Routes} from "@angular/router";
import {MatIconModule} from "@angular/material/icon";
import { ProjectDisclosureComponent } from '../disclosure/project-disclosure/project-disclosure.component';
import { CoiService } from '../disclosure/services/coi.service';
import { DataStoreService } from '../disclosure/services/data-store.service';
import { ElasticConfigService } from 'projects/fibi/src/app/common/services/elastic-config.service';
import { SharedModule } from '../shared/shared.module';

const routes: Routes = [
    {path: '', redirectTo: 'list/disclosures', pathMatch: 'full'},
    {
        path: 'list', component: UserDashboardComponent,
        children: [{
            path: 'disclosures',
            loadChildren: () => import('./user-disclosure/user-disclosure.module').then(m => m.UserDisclosureModule)
        }, {
            path: 'entities',
            loadChildren: () => import('./user-entities/user-entities.module').then(m => m.UserEntitiesModule)
        }]
    }];

@NgModule({
    declarations: [
        UserDashboardComponent,
        ProjectDisclosureComponent
        
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        SharedModule
    ],
    providers: [CoiService, DataStoreService,
        ElasticConfigService]
})
export class UserDashboardModule {
}
