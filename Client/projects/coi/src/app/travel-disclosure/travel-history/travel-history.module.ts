import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TravelHistoryComponent } from './travel-history.component';
import { SharedComponentModule } from '../../shared-components/shared-component.module';
import { FormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';

const routes: Routes = [
    {
        path: '', component: TravelHistoryComponent,
    }
];

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        FormsModule,
        RouterModule.forChild(routes),
        SharedComponentModule,
        MatIconModule
    ],
    declarations: [
        TravelHistoryComponent
    ]
})
export class TravelHistoryModule { }
