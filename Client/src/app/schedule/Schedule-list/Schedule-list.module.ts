import { FormsModule } from '@angular/forms';
import { SharedModule } from './../../shared/shared.module';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ScheduleListComponent } from './Schedule-list.component';
import {RouterModule, Routes} from '@angular/router';
import { ScheduleListService } from './Schedule-list.service';

const routes: Routes = [
  {path: '', component: ScheduleListComponent}
];

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    RouterModule.forChild(routes)
  ],
  declarations: [ScheduleListComponent],
  providers: [ScheduleListService]
})
export class ScheduleListModule { }
