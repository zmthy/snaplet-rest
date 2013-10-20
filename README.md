REST resources for the Snap framework.

```haskell
import Data.CaseInsensitive (CI)
import Snap.Snaplet.Rest
```

As an example, let's translate the following datatype into a resource.

```haskell
data User = User { user :: Username, name :: String, age :: Int }
deriveJSON ''User

type Username = CI String
```

We need a type to represent changes to the resource.  This 'partial' type
indicates what properties to change: either the name, the age, or both.

```haskell
data UserPart = UserPart { partName :: Maybe String, partAge :: Mabe Int }
```

This type also acts as a search mechanism: we can search by names, ages, or
both.  We can use either a username or a `UserPart` search to find users, and
define a function to convert URL query string parameters to this search.

```haskell
type UserId = Either Username UserPart

userIdFromParams :: Params -> Maybe UserId
```

Now we have the pieces required to define our CRUD behaviour.

```haskell
createUser :: User -> AppHandler ()

readUser :: UserId -> AppHandler [User]

updateUser :: UserId -> UserPart -> AppHandler Bool

deleteUser :: UserId -> AppHandler Bool
```

Because we derived JSON instances, we can add JSON as a media format without
having to define these manually.  Once the behaviour is attached to the
resource, it can be served in the handler.

```haskell
serveUser :: AppHandler ()
serveUser = serveResource $ resource
    & addMedia jsonInstances
    & setCreate createUser
    & setRead readUser
    & setUpdate updateUser
    & setDelete deleteUser
    & setFromParams userIdFromParams
```

