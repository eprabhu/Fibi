import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ReviewerDashboardComponent } from './reviewer-dashboard.component';
import { MatIconModule } from '@angular/material/icon';
import { Routes, RouterModule } from '@angular/router';
import { SharedModule } from '../shared/shared.module';
import { ReviewerDashboardService } from './reviewer-dashboard.service';
import { FormsModule } from '@angular/forms';
import { SharedComponentModule } from '../shared-components/shared-component.module';

const routes: Routes = [{path: '', component: ReviewerDashboardComponent}];

@NgModule({
  imports: [
    CommonModule,
    RouterModule.forChild(routes),
    SharedModule,
    MatIconModule,
    FormsModule,
    SharedComponentModule
  ],
  declarations: [ReviewerDashboardComponent],
  providers: [ReviewerDashboardService]
})
export class ReviewerDashboardModule { }
