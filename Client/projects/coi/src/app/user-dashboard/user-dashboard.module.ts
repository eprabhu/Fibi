import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {UserDashboardComponent} from './user-dashboard.component';
import {RouterModule, Routes} from "@angular/router";
import {MatIconModule} from "@angular/material/icon";
import { CoiService } from '../disclosure/services/coi.service';
import {SharedModule} from "../shared/shared.module";
import {FormsModule} from "@angular/forms";
import { SfiService } from '../disclosure/sfi/sfi.service';
import { SharedComponentModule } from '../shared-components/shared-component.module';

const routes: Routes = [
    // {path: '', redirectTo: 'disclosures', pathMatch: 'full'},
    {
        path: '', component: UserDashboardComponent,
        children: [{ path: '', redirectTo: 'disclosures', pathMatch: 'full' },
        {
            path: 'disclosures',
            loadChildren: () => import('./user-disclosure/user-disclosure.module').then(m => m.UserDisclosureModule)
        }, {
            path: 'entities',
            loadChildren: () => import('./user-entities/user-entities.module').then(m => m.UserEntitiesModule)
        }, {
            path: 'notes',
            loadChildren: () => import('./notes/notes-attachments.module').then(m => m.NotesAttachmentsModule)
        }, {
            path: 'attachments',
            loadChildren: () => import('./attachments/attachments.module').then(m => m.AttachmentsModule)
        }]
    }];

@NgModule({
    declarations: [
        UserDashboardComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        SharedModule,
        FormsModule,
        SharedComponentModule
    ],
    providers: [CoiService, SfiService]
})
export class UserDashboardModule {
}
