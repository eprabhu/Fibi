import { SharedModule } from './../../shared/shared.module';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { CommiteeListComponent } from './commitee-list.component';
import {RouterModule, Routes} from '@angular/router';
import { FormsModule } from '@angular/forms';
import { CommiteeListService } from './commitee-list.service';

const routes: Routes = [
  {path: '', component: CommiteeListComponent}
];
@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    RouterModule.forChild(routes)
  ],
  declarations: [CommiteeListComponent],
  providers: [CommiteeListService]
})
export class CommiteeListModule { }
