import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelRelatedDisclosureComponent } from './travel-related-disclosures.component';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';
import { AddSfiModule } from '../../add-sfi/add-sfi.module';

const routes: Routes = [
    {
        path: '', component: TravelRelatedDisclosureComponent,
    }
];

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        AddSfiModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
    ],
    declarations: [
        TravelRelatedDisclosureComponent
    ]
})
export class TravelRelatedDisclosuresModule { }
