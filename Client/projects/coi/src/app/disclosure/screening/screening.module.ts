import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {ScreeningComponent} from './screening.component';
import {RouterModule, Routes} from "@angular/router";

const routes: Routes = [{path: '', component: ScreeningComponent}];

@NgModule({
    declarations: [
        ScreeningComponent
    ],
    imports: [
        CommonModule,
        RouterModule.forChild(routes)
    ]
})
export class ScreeningModule {
}
