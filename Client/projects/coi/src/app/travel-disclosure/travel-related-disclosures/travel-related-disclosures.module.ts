import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelRelatedDisclosureComponent } from './travel-related-disclosures.component';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';
import { TravelSharedComponentModule } from '../travel-shared-component/travel-shared-component.module';

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
        RouterModule.forChild(routes),
        SharedComponentModule,
        TravelSharedComponentModule
    ],
    declarations: [
        TravelRelatedDisclosureComponent
    ]
})
export class TravelRelatedDisclosuresModule { }
