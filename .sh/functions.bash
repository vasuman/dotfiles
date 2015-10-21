function github {
    case $1 in
        "create")
            repo_name=$2
            uname=$GITHUB_USER
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
        "cd")
            dir=$2
            cd $GITHUB_DIR/$dir
            ;;
        *)
            echo "github: unknown command $1"
            exit 1
            ;;
    esac
}

function glob {
    glob_dir=$GITHUB_DIR/vasuman/glob/
    out_dir=$GITHUB_DIR/$GITHUB_USER/$GITHUB_USER.github.io/
    port=8000
    if [[ ! -d $glob_dir ]]; then
        echo "blog: get glob first"
        exit 1
    fi
    if [[ $1 == "-deploy" ]]; then
        python2 $glob_dir/generate.py $BLOG_DIR $out_dir -skipdrafts
        cd $out_dir
        git add . 
        git commit -am 'Automated commit' 
        git push
        cd -
    elif [[ $1 == "-watch" ]]; then
        cmd=''
        echo "Watching... $BLOG_DIR"
        python2 $glob_dir/generate.py $BLOG_DIR $out_dir
        while inotifywait -r -e close_write,moved_to,create "$BLOG_DIR"; do
            python2 $glob_dir/generate.py $BLOG_DIR $out_dir
        done
    fi
}
