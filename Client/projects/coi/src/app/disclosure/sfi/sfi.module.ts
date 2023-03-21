import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {SfiComponent} from './sfi.component';
import {RouterModule, Routes} from "@angular/router";

const routes: Routes = [{path: '', component: SfiComponent}];

@NgModule({
    declarations: [
        SfiComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ]
})
export class SfiModule {
}
