import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AddSfiComponent } from './add-sfi.component';
import { FormsModule } from '@angular/forms';
import { RouterModule, Routes } from '@angular/router';
import { SharedModule } from '../shared/shared.module';
import { SharedEntityInfoCardComponent } from './shared-entity-info-card/shared-entity-info-card.component';
import { SfiDataResolveGuardService } from './services/sfi-data-resolve-guard.service';


const routes: Routes = [
  {
    path: 'create', component: AddSfiComponent, resolve: {moduleConfig: SfiDataResolveGuardService}
  }];

@NgModule({
  imports: [
    CommonModule,
    FormsModule,
    SharedModule,
    RouterModule.forChild(routes),
  ],
  declarations: [AddSfiComponent,SharedEntityInfoCardComponent],
  exports: [AddSfiComponent,SharedEntityInfoCardComponent],
  providers: [SfiDataResolveGuardService]
})
export class AddSfiModule { }
