import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelDisclosureFormComponent } from './travel-disclosure-form.component';
import { RouterModule, Routes } from '@angular/router';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { SharedModule } from '../../shared/shared.module';
import { AddSfiModule } from '../../add-sfi/add-sfi.module';

const routes: Routes = [
    {
        path: '', component: TravelDisclosureFormComponent,
    }
];
@NgModule({
    declarations: [
        TravelDisclosureFormComponent
    ],
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
        AddSfiModule
    ],
    providers: [],
    exports: []
})
export class TravelDisclosureFormModule { }
