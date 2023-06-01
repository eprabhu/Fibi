import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouterModule } from '@angular/router';
import {TravelCertificationComponent} from './travel-certification.component';
import {SharedModule} from '../../shared/shared.module';


@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        RouterModule.forChild([{ path: '', component: TravelCertificationComponent }])
    ],
    declarations: [TravelCertificationComponent]
})
export class TravelCertificationModule { }
