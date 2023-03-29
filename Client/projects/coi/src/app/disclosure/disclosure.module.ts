import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {DisclosureComponent} from './disclosure.component';
import {RouterModule, Routes} from "@angular/router";
import {MatButtonModule} from "@angular/material/button";
import {MatIconModule} from "@angular/material/icon";

import {AddSfiComponent} from './sfi/add-sfi/add-sfi.component';
import {SfiService} from './sfi/sfi.service';
import {SearchFieldComponent} from './sfi/search-field/search-field.component';

import {ResolveServiceService} from "./services/resolve-service.service";
import {CoiService} from "./services/coi.service";
import {DataStoreService} from "./services/data-store.service";
import {RouterGuardService} from "./services/router-guard.service";
import {SharedModule} from "../shared/shared.module";
import { SfiModule } from './sfi/sfi.module';
import { AddRelationshipModalComponent } from './sfi/Add-relationship-modal/Add-relationship-modal.component';

const routes: Routes = [
    {
        path: '', component: DisclosureComponent, canActivate: [ResolveServiceService],
        children: [
            {
                path: 'screening',
                loadChildren: () => import('./screening-questionnaire/screening-questionnaire.module').then(m => m.ScreeningQuestionnaireModule)
            },
            {
                path: 'sfi', loadChildren: () => import('./sfi/sfi.module').then(m => m.SfiModule)
            },
            {
                path: 'relationship',
                loadChildren: () => import('./relationship/relationship.module').then(m => m.RelationshipModule)
            },
            {
                path: 'certification',
                loadChildren: () => import('./certification/certification.module').then(m => m.CertificationModule)
            },
            {
                path: 'summary', loadChildren: () => import('./summary/summary.module').then(m => m.SummaryModule)
            },
            {
                path: 'conflict-management',
                loadChildren: () => import('./attachment/attachment.module').then(m => m.AttachmentModule)
            },
            {
                path: 'review', loadChildren: () => import('./review/review.module').then(m => m.ReviewModule)
            },
            {
                path: 'history', loadChildren: () => import('./history/history.module').then(m => m.HistoryModule)
            }
        ]
    }];

@NgModule({
    declarations: [
        DisclosureComponent,
        AddSfiComponent,
        SearchFieldComponent,
        AddRelationshipModalComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        MatButtonModule,
        SharedModule, SfiModule
    ],
    providers: [
        SfiService,
        ResolveServiceService,
        DataStoreService,
        CoiService,
        RouterGuardService
    ]
})
export class DisclosureModule {
}
