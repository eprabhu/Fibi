import { FormsModule } from '@angular/forms';
import { SharedModule } from './../../shared/shared.module';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { DisclosureListComponent } from './disclosure-list.component';
import {RouterModule, Routes} from '@angular/router';
import { DisclosureListService } from './disclosure-list.service';

const routes: Routes = [
  {path: '', component: DisclosureListComponent}
];

@NgModule({
  imports: [
    CommonModule,
    SharedModule,
    FormsModule,
    RouterModule.forChild(routes)
  ],
  declarations: [DisclosureListComponent],
  providers:[DisclosureListService]
})
export class DisclosureListModule { }
