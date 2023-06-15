import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelDisclosureComponent } from './travel-disclosure.component';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { SharedModule } from '../shared/shared.module';
import { TravelDisclosureService } from './services/travel-disclosure.service';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { TravelRouteGuardService } from './services/travel-route-guard.service';
import { FormsModule } from '@angular/forms';
import { Routes, RouterModule } from '@angular/router';
import { TravelDataStoreService } from './services/travel-data-store.service';
import { CoiService } from '../disclosure/services/coi.service';

const routes: Routes = [
    {
        path: '', component: TravelDisclosureComponent, canActivate: [TravelRouteGuardService], canDeactivate: [TravelRouteGuardService],
        children: [
            {
                path: 'travel-details', canDeactivate: [TravelRouteGuardService],
                loadChildren: () => import('./travel-disclosure-form/travel-disclosure-form.module').then(m => m.TravelDisclosureFormModule)
            },
            {
                path: 'certification', canDeactivate: [TravelRouteGuardService],
                loadChildren: () => import('./travel-certification/travel-certification.module').then(m => m.TravelCertificationModule)
            },
            {
                path: 'screening', canDeactivate: [TravelRouteGuardService],
                loadChildren: () => import('./travel-questionnaire/travel-questionnaire.module')
                .then(m => m.TravelQuestionnaireModule)
            },
            {
                path: 'summary', canDeactivate: [],
                loadChildren: () => import('./travel-summary/travel-summary.module')
                .then(m => m.TravelSummaryModule)
            }
        ]
    }
];
@NgModule({
    declarations: [
        TravelDisclosureComponent
    ],
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
    ],
    providers: [
        SfiService,
        TravelDisclosureService,
        TravelRouteGuardService,
        TravelDataStoreService,
        CoiService
    ],
    exports: []
})
export class TravelDisclosureModule {
}
