import {NgModule} from '@angular/core';
import {CommonModule} from '@angular/common';
import {SfiComponent} from './sfi.component';
import {RouterModule, Routes} from "@angular/router";
import { AdditionalSfiDetailsComponent } from './additional-sfi-details/additional-sfi-details.component';
import {SharedModule} from "../../shared/shared.module";
import {FormsModule} from "@angular/forms";

const routes: Routes = [{path: '', component: SfiComponent}];
@NgModule({
    declarations: [
        SfiComponent,
        AdditionalSfiDetailsComponent
    ],
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild(routes),
    ],
})
export class SfiModule {
}
