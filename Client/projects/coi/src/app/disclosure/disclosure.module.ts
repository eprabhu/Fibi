import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {DisclosureComponent} from './disclosure.component';
import {RouterModule, Routes} from '@angular/router';
import {MatButtonModule} from '@angular/material/button';
import {MatIconModule} from '@angular/material/icon';

import {SfiService} from './sfi/sfi.service';
import {SearchFieldComponent} from './sfi/search-field/search-field.component';

import {ResolveServiceService} from './services/resolve-service.service';
import {RouterGuardService} from './services/router-guard.service';
import {SharedModule} from '../shared/shared.module';
import { FormsModule } from '@angular/forms';
import { SfiModule } from './sfi/sfi.module';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import {DataStoreService} from "./services/data-store.service";
import { SharedDisclosureModule } from './shared-disclosure/shared-disclosure.module';
import { EntityRiskSliderModule } from './entity-risk-slider/entity-risk-slider.module';
import { HeaderSlidersModule } from './header-sliders/header-sliders.module';
import { SfiListComponent } from './sfi-list/sfi-list.component';


const routes: Routes = [
    {
        path: '', component: DisclosureComponent, canActivate: [ResolveServiceService],
        children: [
            {
                path: 'screening', canDeactivate: [RouterGuardService],
                loadChildren: () => import('./screening-questionnaire/screening-questionnaire.module').then(m => m.ScreeningQuestionnaireModule)
            },
            {
                path: 'sfi', canDeactivate: [RouterGuardService], component: SfiListComponent
            },
            {
                path: 'relationship', canDeactivate: [RouterGuardService],
                loadChildren: () => import('./relationship/relationship.module').then(m => m.RelationshipModule)
            },
            {
                path: 'certification', canActivate: [RouterGuardService], canDeactivate: [RouterGuardService],
                loadChildren: () => import('./certification/certification.module').then(m => m.CertificationModule)
            },
            {
                path: 'summary', canDeactivate: [RouterGuardService],
                loadChildren: () => import('./summary/summary.module').then(m => m.SummaryModule)
            },
            {
                path: 'conflict-management', canDeactivate: [RouterGuardService],
                loadChildren: () => import('./attachment/attachment.module').then(m => m.AttachmentModule)
            },
            {
                path: 'review', canDeactivate: [RouterGuardService], canActivate: [ResolveServiceService],
                loadChildren: () => import('./review/review.module').then(m => m.ReviewModule)
            },
            {
                path: 'history', canDeactivate: [RouterGuardService],
                loadChildren: () => import('./history/history.module').then(m => m.HistoryModule)
            }
        ]
    }];

@NgModule({
    declarations: [
        DisclosureComponent,
        SearchFieldComponent,
        SfiListComponent
    ],
    providers: [
        SfiService,
        DataStoreService,
        ResolveServiceService,
        RouterGuardService,
    ],
    exports: [],
    imports: [
        CommonModule,
        RouterModule.forChild(routes),
        MatIconModule,
        MatButtonModule,
        SharedModule,
        SfiModule,
        FormsModule,
        SharedComponentModule,
        SharedDisclosureModule,
        EntityRiskSliderModule,
        HeaderSlidersModule
    ]
})
export class DisclosureModule {
}
