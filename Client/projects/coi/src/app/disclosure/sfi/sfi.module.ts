import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { SfiComponent } from './sfi.component';
import { RouterModule, Routes } from "@angular/router";
import { AdditionalSfiDetailsComponent } from './additional-sfi-details/additional-sfi-details.component';

const routes: Routes = [{ path: '', component: SfiComponent }];
@NgModule({
  declarations: [
    SfiComponent,
    AdditionalSfiDetailsComponent
  ],
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
  ]
})
export class SfiModule {
}
