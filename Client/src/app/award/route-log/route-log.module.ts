import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { RouteLogComponent } from './route-log.component';
import { RouterModule } from '@angular/router';
import { SharedModule } from '../../shared/shared.module';
import { FormsModule } from '@angular/forms';

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild([{ path: '', component: RouteLogComponent }]),
    SharedModule,
    FormsModule
  ],
  declarations: [RouteLogComponent]
})
export class RouteLogModule { }
