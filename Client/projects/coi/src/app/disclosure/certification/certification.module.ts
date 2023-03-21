import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {CertificationComponent} from './certification.component';
import {RouterModule, Routes} from "@angular/router";

const routes: Routes = [{path: '', component: CertificationComponent}];

@NgModule({
    declarations: [
        CertificationComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ]
})
export class CertificationModule {
}
