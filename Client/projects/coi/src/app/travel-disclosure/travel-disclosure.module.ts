import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule, Routes } from "@angular/router";
import { FormsModule } from "@angular/forms";
import { TravelDisclosureComponent } from './travel-disclosure.component';
import { CoiService } from '../disclosure/services/coi.service';
import { DataStoreService } from '../disclosure/services/data-store.service';
import { SharedComponentModule } from '../shared-components/shared-component.module';
import { SharedModule } from '../shared/shared.module';
import { TravelDisclosureService } from './travel-disclosure.service';
import { SfiService } from '../disclosure/sfi/sfi.service';
import { travelRouteGuardService } from './travel-route-guard.service';

const routes: Routes = [
    {
        path: '', component: TravelDisclosureComponent,
        children: [
            {
                path: 'travel-details', canDeactivate: [travelRouteGuardService],
                loadChildren: () => import('./travel-disclosure-form/travel-disclosure-form.module').then(m => m.TravelDisclosureFormModule)
            },
            {
                path: 'certification', canDeactivate: [travelRouteGuardService],
                loadChildren: () => import('../shared-components/certification/certification.module').then(m => m.CertificationModule)
            },
            {
                path: 'screening', canDeactivate: [travelRouteGuardService],
                loadChildren: () => import('./screening-questionnaire/screening-questionnaire.module').then(m => m.ScreeningQuestionnaireModule)
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
        SharedComponentModule
    ],
    providers: [
        SfiService,
        DataStoreService,
        CoiService,
        TravelDisclosureService,
        travelRouteGuardService
    ],
    exports: []
})
export class TravelDisclosureModule {
}
