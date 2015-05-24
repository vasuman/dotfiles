export GITHUB_DIR=$CODE_DIR/github.com
function github {
    case $1 in
        "create")
            repo_name=$2
            uname=`git config github.user`
            token=`git config github.token`
            echo "creating $uname/$repo_name"
            curl -u "$username:$token" https://api.github.com/user/repos -d '{"name":"'$repo_name'"}'
            if [ -d .git ]; then
                git remote add origin https://$uname@github.com/$uname/$repo_name.git
            fi
            ;;
        "get")
            repo=$2
            dir=$GITHUB_DIR/$repo
            mkdir -p $dir
            git clone https://github.com/$repo $dir
            ;;
        *)
            echo "github: unknown command $1"
            return 1
            ;;
    esac
}
