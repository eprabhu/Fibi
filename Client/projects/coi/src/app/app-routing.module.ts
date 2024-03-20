import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AppRouterComponent } from "./common/app-router/app-router.component";
import { DashboardGuardService } from './common/services/dashboard-guard.service';
import { AdminRouteGuardService } from './common/services/guards/admin-route-guard.service';
import { LoginGuard } from './common/services/guards/login-guard.service';

const routes: Routes = [
    {
        path: '', redirectTo: 'coi', pathMatch: 'full'
    },
    {
        path: 'coi', component: AppRouterComponent, canActivate: [DashboardGuardService], children: [
            { path: '', redirectTo: 'user-dashboard', pathMatch: 'full' },
            {
                path: 'disclosure',
                loadChildren: () => import('./disclosure/disclosure.module').then(m => m.DisclosureModule)
            },
            {
                path: 'create-disclosure',
                loadChildren: () => import('./disclosure/disclosure.module').then(m => m.DisclosureModule)
            },
            {
                path: 'reviewer-dashboard',
                loadChildren: () => import('./reviewer-dashboard/reviewer-dashboard.module').then(m => m.ReviewerDashboardModule)
            },
            {
                path: 'user-dashboard',
                loadChildren: () => import('./user-dashboard/user-dashboard.module').then(m => m.UserDashboardModule)
            },
            {
                path: 'admin-dashboard',
                loadChildren: () => import('./admin-dashboard/admin-dashboard.module').then(m => m.AdminDashboardModule),
                canActivate: [AdminRouteGuardService]

            },
            {
                path: 'entity-management',
                loadChildren: () => import('./entity-management/entity-management.module').then(m => m.EntityManagementModule),

            },
            {
                path: 'entity-details', loadChildren: () => import('../app/disclosure/entity-details/entity-details.module').then(m => m.EntityDetailsModule)
            },
            {
                path: 'expanded-widgets',
                loadChildren: () => import('./common/header/expanded-widgets/expanded-action-list/expanded-widgets.module').then(m => m.ExpandedActionListModule)
            },
            {
                path: 'user',
                loadChildren: () => import('./user/user.module').then(m => m.UserModule)
            },
            {
                path: 'create-travel-disclosure',
                loadChildren: () => import('./travel-disclosure/travel-disclosure.module').then(m => m.TravelDisclosureModule)
            },
            {
                path: 'travel-disclosure',
                loadChildren: () => import('./travel-disclosure/travel-disclosure.module').then(m => m.TravelDisclosureModule)
            },
            {
                path: 'error-handler',
                loadChildren: () => import('./error-handler/error-handler.module').then(m => m.ErrorHandlerModule)
            },
            {
              path: 'configuration',
              loadChildren: () => import('./configuration/configuration.module').then(m => m.ConfigurationModule)
            },
            {
                path: 'create-sfi',
                loadChildren: () => import('./add-sfi/add-sfi.module').then(m => m.AddSfiModule)
            },
            {
                path: 'opa',
                loadChildren: () => import('./opa/opa.module').then(m => m.OpaModule)
            },
            {
                path: 'opa-dashboard',
                loadChildren: () => import('./opa-dashboard/opa-dashboard.module').then(m => m.OpaDashboardModule)
            },
          ]
    },
{ path: 'login', loadChildren: () => import('./login/login.module').then(m => m.LoginModule), canActivate: [LoginGuard] }
];

@NgModule({
    imports: [RouterModule.forRoot(routes)],
    exports: [RouterModule]
})
export class AppRoutingModule {
}
