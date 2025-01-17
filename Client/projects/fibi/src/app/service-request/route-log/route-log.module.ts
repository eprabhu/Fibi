import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouteLogComponent } from './route-log.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';

@NgModule({
    imports: [
        CommonModule,
        SharedModule,
        RouterModule.forChild(
            [
                { path: '', component: RouteLogComponent }
            ]
        )
    ],
    declarations: [RouteLogComponent]
})
export class RouteLogModule { }
