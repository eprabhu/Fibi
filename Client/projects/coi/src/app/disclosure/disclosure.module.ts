import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {DisclosureComponent} from './disclosure.component';
import {RouterModule, Routes} from "@angular/router";

const routes: Routes = [{
    path: '', component: DisclosureComponent,
    children: [
        {path: 'screening', loadChildren: () => import('./screening/screening.module').then(m => m.ScreeningModule)},
        {path: 'sfi', loadChildren: () => import('./sfi/sfi.module').then(m => m.SfiModule)},
        {
            path: 'relationship',
            loadChildren: () => import('./relationship/relationship.module').then(m => m.RelationshipModule)
        },
        {
            path: 'certification',
            loadChildren: () => import('./certification/certification.module').then(m => m.CertificationModule)
        },
    ]
}];

@NgModule({
    declarations: [
        DisclosureComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ]
})
export class DisclosureModule {
}
